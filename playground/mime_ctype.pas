{   $Id$

   OpenXP MIME Library: Content Types
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

unit mime_ctype;

{ ---------------------------} interface { --------------------------- }

uses
  Classes;

type

  {
    This class holds parameter values
  }

  TMimeParam = class
  public
    Value,Charset,Language: String;
    constructor Create(Const NValue,NCharset,NLanguage: String);
  end;

  {
    This abstract base class implements the basic MIME Content-* header
    parameter handling. The first word (Verb) is handeled by dereived
    classes.
  }

  TMimeContentHeader_AbstractBaseClass = class
  private
    FParam:     TStringList;

    function    MayEncodeParam(const name:string):boolean; virtual; abstract;

    function    GVerb:string; virtual; abstract;
    procedure   SVerb(const value:string); virtual; abstract;

    function    GAsString:String;
    procedure   SAsString(Const NewValue:String);

    function    GParam(Const Name:String):TMimeParam;

    function    GParamValue(Const Name:String):String;
    procedure   SParamValue(Const Name:String;Const NewValue:String);

  public
    constructor Create(const ctype:String);
    destructor  Destroy; override;

    property    Verb: String read GVerb write SVerb;

    property    Params[Const Name: String]: TMimeParam read GParam;
    property    ParamValues[Const Name: String]: String read GParamValue write SParamValue;

    property    AsString: String   read GAsString  write SAsString;
    function    AsFoldedString(MaxFirstLen,MaxLen:Integer;UseRFC2231:Boolean):String;
  end;

  {
    This class implements the handling of the MIME Content-Type header
    field contents
  }

  TMimeContentType = class(TMimeContentHeader_AbstractBaseClass)
  private
    FMainType:  String;
    FSubType:   String;

    function    MayEncodeParam(const name:string):boolean;     override;
    function    GVerb:string;                                  override;
    procedure   SVerb(const value:string);                     override;

    function    GCharset:string;
    procedure   SCharset(const value:string);

    function    GNeedCharset:Boolean;
    function    GIsEncodeable:Boolean;

  public
    property MainType: String   read FMainType  write FMainType;
    property SubType:  String   read FSubType   write FSubType;

    property Charset:  String   read GCharset   write SCharset;

    property NeedCharset:Boolean read GNeedCharset;
    property IsEncodeable:Boolean read GIsEncodeable;
  end;

function MimeContentTypeNeedCharset(const ctype:string):Boolean;
function MimeContentTypeIsEncodeable(const ctype:string):Boolean;

procedure MimeContentTypeSplit(const ctype:string; var main,sub:string);

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
  raise Exception.Create('not implemented');
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
begin
  MimeContentTypeSplit(Value,FMainType,FSubType);
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

function TMimeContentType.GNeedCharset:Boolean;
begin
  result:=MimeContentTypeNeedCharset(Verb);
end;

function TMimeContentType.GIsEncodeable:Boolean;
begin
  result:=MimeContentTypeIsEncodeable(Verb);
end;

procedure MimeContentTypeSplit(const ctype:string; var main,sub:string);
var j,e: Integer;
    s: string;
begin
  if ctype='' then begin
    Main:='';
    Sub :='';
    exit;
  end;

  e := Pos(';',CType); if e=0 then e:=Length(Ctype);

  j := Pos('/',CType); if j>e then j:=0;

  if j<=0 then begin
    s:=LowerCase(Ctype);

    if (s='tex') or (s='troff') then
    begin
      Main:='text'; Sub:='x-'+Copy(CType,1,e);
    end else
    if (s='postscript') or (s='sgml') or (Copy(s,1,2)='x-') then
    begin
      Main:='application'; Sub:=Copy(CType,1,e);
    end else
    begin
      Main:='application'; Sub:='x-'+Copy(CType,1,e);
    end;
  end else
  begin
    Main := Trim(Copy(CType,1,j-1));
    Sub  := Trim(Copy(CType,j+1,e-j));
  end;
end;

function MimeContentTypeNeedCharset(const ctype:string):Boolean;
var m,s: string;
begin
  MimeContentTypeSplit(ctype,m,s);
  m:=Lowercase(m);
  s:=Lowercase(s);

  result:= ((m='text') and not( (s='parityfec') or
                                (s='rfc822-headers') or
                                (s='rtf') or
                                (s='t140') or
                                (s='vnd.dmclientscript') or
                                (s='vnd.fly') or
                                (s='vnd.latex-z') or
                                (s='vnd.ms-mediapackage') or
                                (s='vnd.motorola.reflex') or
                                (s='prs.lines.tag') )) or
     ((m='application') and   ( (s='edi-consent') or
                                (s='edi-x12') or
                                (s='edifact') or
                                (s='prs.alvestrand.titrax-sheet') or
                                (s='sgml-open-catalog') or
                                (s='vnd.commerce-battelle') or
                                (s='vnd.dpgraph') or
                                (s='vnd.mozilla.xul+xml') or
                                (s='vnd.msign') or
                                (s='vnd.uplanet.alert') or
                                (s='vnd.uplanet.alert-wbxml') or
                                (s='vnd.uplanet.bearer-choice-wbxml') or
                                (s='vnd.uplanet.bearer-choice') or
                                (s='vnd.uplanet.cacheop') or
                                (s='vnd.uplanet.cacheop-wbxml') or
                                (s='vnd.uplanet.channel') or
                                (s='vnd.uplanet.channel-wbxml') or
                                (s='vnd.uplanet.list') or
                                (s='vnd.uplanet.list-wbxml') or
                                (s='vnd.uplanet.listcmd') or
                                (s='vnd.uplanet.listcmd-wbxml') or
                                (s='vnd.vnd.wap.sic') or
                                (s='vnd.vnd.wap.slc') or
                                (s='vnd.wap.wbxml') or
                                (s='vnd.wap.wmlc') or
                                (s='vnd.wap.wmlscriptc') or
                                (s='beep+xml') or
                                (s='iotp') or
                                (s='xml') or
                                (s='xml-external-parsed-entity') or
                                (s='xml-dtd') )) or
     ((m='image') and         ( (s='vnd.wap.wbmp') ));
end;                             

function MimeContentTypeIsEncodeable(const ctype:string):Boolean;
var m,s: string;
begin
  MimeContentTypeSplit(ctype,m,s);
  m:=Lowercase(m);
  s:=Lowercase(s);

  result := (m<>'message') and (m<>'multipart') and
    ((m<>'application') or (m<>'mac-binhex40'));
end;

end.