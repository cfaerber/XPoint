{   $Id$

   OpenXP MIME Library: Content Disposition
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

{$I xpdefine.inc}

unit mime_disposition;

{ ---------------------------} interface { --------------------------- }

uses
  Classes, Mime_CType;

type
  {
    This class implements the handling of the MIME Content-Type header
    field contents
  }

  TMimeDisposition

  TMimeDisposition = class(TMimeContentHeader_AbstractBaseClass)
  private
    FMainType:  String;
    FSubType:   String;

    function    MayEncodeParam(const name:string):boolean;     override;
    function    GVerb:string;                                  override;
    procedure   SVerb(const value:string);                     override;

    function    GCharset:string;
    procedure   SCharset(const value:string);

    function    GIsLineBased:boolean;
    function    GIsEncodeable:boolean;

  public
    property MainType: String   read FMainType  write FMainType;
    property SubType:  String   read FSubType   write FSubType;

    property Charset:  String   read GCharset   write SCharset;

    property IsLineBased:Boolean read GIsLineBased;
    property IsEncodeable:Boolean read GIsEncodeable;
  end;

{ ------------------------} implementation { ------------------------- }

uses
  SysUtils;

constructor TMimeParam.Create(Const NValue,NCharset,
  NLanguage: String);
begin
  Value   :=NValue;
  Charset :=NCharset;
  Language:=NLanguage;
end;

constructor TMimeContentHeader_AbstractBaseClass.Create(const ctype:String);
begin
  inherited Create;
  FParam := TStringList.Create;
  FParam.CaseSensitive := False;
  AsString := ctype;
end;

destructor TMimeContentHeader_AbstractBaseClass.Destroy;
var i: integer;
begin
  for i:=0 to FParam.Count do
    FParam.Objects[i].Free;
  FParam.Free;
  inherited Destroy;
end;

function TMimeContentHeader_AbstractBaseClass.GAsString:String;
var i: Integer;

  function Encode(param: String):String;
  var i: Integer;
      e: Boolean;
  begin
    e:=false;
    Result:='';

    for i:=1 to Length(param) do
      if param[i] in ['(', ')', '<', '>', '@', ',', ';', ':', '/', '[', ']', '?', '=',' '] then begin
        e:=true;
        Result:=Result+param[i];
      end else
      if param[i] in [#0..#31,'"','\'] then begin
        e:=true;
        Result:=Result+'\'+param[i];
      end else
        Result:=Result+param[i];

    if e then Result := '"'+Result+'"';
  end;

begin
  for i := FParam.Count-1 downto 0 do
    if Length((FParam.Objects[i] as TMimeParam).Value)<=0 then begin
      FParam.Objects[i].Free;
      FParam.Delete(i);
    end;

  Result := Verb;

  for i:= 0 to FParam.Count-1 do
    Result:=Result+'; '+Encode(FParam.Strings[i])+'='+Encode(
      (FParam.Objects[i] as TMimeParam).Value);
end;

function TMimeContentHeader_AbstractBaseClass.AsFoldedString(MaxFirstLen,MaxLen:Integer;UseRFC2231:Boolean):String;
begin

end;

procedure TMimeContentHeader_AbstractBaseClass.SAsString(Const NewValue:String);
var i,j: Integer;
    p,v: String;
    q,s: boolean;
begin
  for i:= FParam.Count-1 downto 0 do begin
    FParam.Objects[i].Free;
    FParam.Delete(i);
  end;

  j := Pos(';',NewValue);

  if j >0 then
    Verb := Trim(Copy(NewValue,1,j-1))
  else
    Verb := Trim(NewValue);

  inc(j);
  i:=j;

  while j<=Length(NewValue) do
  begin
    if NewValue[j]='=' then
    begin
      p:=Trim(Copy(NewValue,i,j-i));
      q:=false;
      s:=false;
      v:='';
      inc(j);
      while j<=Length(NewValue) do
      begin
        if NewValue[j]='"' then begin   (* handle quotes *)
          q:=not q;
        end else
        if NewValue[j]='\' then begin   (* handle backslash escapes *)
          if s then begin v:=v+' '; s:=false; end;
          inc(j);
          if j<=Length(NewValue) then
            v:=v+NewValue[j];
        end else
        if (NewValue[j] in [' ',#9,#10,#13]) and not q then begin
          if v<>'' then
            s:=true;
        end else
        if (NewValue[j] in [';']) and not q then begin
          break;
        end else begin
          if s then begin v:=v+' '; s:=false; end;
          v:=v+NewValue[j];
        end;
        inc(j);
      end;

      ParamValues[p]:=v;
      i:=j+1;
    end;
    inc(j);
  end;
end;

function TMimeContentHeader_AbstractBaseClass.GParam(Const Name:String):TMimeParam;
var pos: Integer;
    par: TMimeParam;
begin
  pos := FParam.IndexOf(Name);
  par := TMimeParam.Create('','','');

  if pos<0 then begin
    par := TMimeParam.Create('','','');
    FParam.AddObject(Name,par);
    Result := par;
  end
  else begin
    Result := FParam.Objects[pos] as TMimeParam;
  end;
end;

function TMimeContentHeader_AbstractBaseClass.GParamValue(Const Name:String):String;
var pos: Integer;
begin
  pos := FParam.IndexOf(Name);
  if pos<0 then
    Result := ''
  else
    Result := (FParam.Objects[pos] as TMimeParam).Value;
end;

procedure TMimeContentHeader_AbstractBaseClass.SParamValue(Const Name:String;Const NewValue:String);
begin
  with Params[Name] do
  begin
    Value := NewValue;
    Charset := '';
    Language := '';
  end;
end;

procedure TMimeContentType.SVerb(Const Value:String);
var j: Integer;
    s: string;
begin
  if Value='' then begin
    MainType:='';
    SubType:='';
    exit;
  end;

  j := Pos('/',Value);

  if j<=0 then begin
    s:=LowerCase(Value);

    if (s='tex') or (s='troff') then
    begin
      MainType:='text'; SubType:='x-'+Value;
    end else
    if (s='postscript') or (s='sgml') or (Copy(s,1,2)='x-') then
    begin
      MainType:='application'; SubType:=Value;
    end else
    begin
      MainType:='application'; SubType:='x-'+Value;
    end;
  end else
  begin
    MainType := Trim(Copy(Value,1,j-1));
    SubType  := Trim(Copy(Value,j+1,Length(Value)-j));
  end;
end;

function TMimeContentType.GVerb:String;
begin
  Result:=MainType+'/'+SubType;
end;

procedure TMimeContentType.SCharset(Const Value:String);
begin
  ParamValues['charset']:=Value;
end;

function TMimeContentType.GCharset:String;
begin
  Result:=ParamValues['charset'];
end;

function TMimeContentType.MayEncodeParam(const name:string):boolean;
var name_lc: string;
begin
  name_lc := LowerCase(name_lc);
  Result :=
    (name_lc<>'boundary') and
    (name_lc<>'charset');
end;

function TMimeContentType.GIsLineBased:boolean;
var main_lc,sub_lc: String;
begin
  if not IsEncodeable then
  begin
    result := true;
    exit
  end;

  main_lc := Lowercase(MainType);
  if (main_lc='text') then
  begin
    result := true;
    exit
  end;

  sub_lc := LowerCase(MainType);
  if ((main_lc='application') and (sub_lc='activemessage'))
  or ((main_lc='application') and (sub_lc='andrew-insert'))
  or ((main_lc='application') and (sub_lc='atomicmail'))
  or ((main_lc='application') and (sub_lc='andrew-insert'))
  or ((main_lc='application') and (sub_lc='batch-smtp'))
  or ((main_lc='application') and (sub_lc='beep+xml'))
  or ((main_lc='application') and (sub_lc='cybercash')) then
  begin
    result := true;
    exit
  end;

  Result:=False;
end;

{
function GHasCharset:Boolean
begin
  'text/*'
  'application/edi-consent'
  'application/edi-fact'
  'application/edi-x12'
end;
}

function TMimeContentType.GIsEncodeable:boolean;
var main_lc,sub_lc: string;
begin
  main_lc := LowerCase(MainType);
  if (main_lc='multipart')
  or (main_lc='message') then
  begin
    result := false;
    exit
  end;

  sub_lc := LowerCase(MainType);
  if ((main_lc='application') and (sub_lc='binhex40')) then
  begin
    result := false;
    exit
  end;

  result := true;
end;

end.
