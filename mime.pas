{ $Id$

  OpenXP: MIME Library
  Copyright (C) 2001 OpenXP team (www.openxp.de) and Claus F"arber

  This file is derieved from parts of the Free Component Library (FCL)
  Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl
  base64 encoder & decoder (c) 1999 Sebastian Guenther

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

unit mime;

{ ---------------------------} interface { --------------------------- }

uses
  Classes,
  xpcharset,
//  xpstreams,
  xpstreams_codec,
  debug;

{ ---------------------- Enum types & Constants ---------------------- }

type
  TMimeEncoding =        ( MimeEncoding7Bit,
                           MimeEncoding8Bit,
                           MimeEncodingBinary,
                           MimeEncodingQuotedPrintable,
                           MimeEncodingBase64,
                           MimeEncodingUnknown );
  TMimeDispositionType = ( MimeDispositionInline,
                           MimeDispositionAttach,
                           MimeDispositionMeta,
                           MimeDispositionNone );
  TMimeEOL =             ( MimeEolCRLF,
                           MimeEolCR,
                           MimeEolLF,
                           MimeEolNone );

const
  MimeEncodingNames: array[TMimeEncoding] of String = (
    '7bit','8bit','binary','quoted-printable','base64','');

{ -------------------- Content & Disposition Types ------------------- }

  { This class holds parameter values                                  }
type
  TMimeParam = class
  public
    Value,Charset,Language: String;
    constructor Create(Const NValue,NCharset,NLanguage: String);
  end;

  { This abstract base class implements the basic MIME Content-* header}
  { parameter handling. The first word (Verb) is handeled by derieved  }
  { classes.                                                           }
  TMimeContentHeader_AbstractBaseClass = class
  private
    FParam:     TStringList;

  protected
    function    MayEncodeParam(const name:string):boolean; virtual; abstract;

    function    GVerb:string; virtual; abstract;
    procedure   SVerb(const value:string); virtual; abstract;

  private
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
    property    ParamNames: TStringList read FParam;

    property    AsString: String   read GAsString  write SAsString;
    function    AsFoldedString(MaxFirstLen,MaxLen:Integer;EOL:String;UseRFC2231:Boolean):String;
  end;

{ --------------------------- Content Types -------------------------- }

  { This class implements the handling of the MIME Content-Type header }
  { field contents                                                     }
  TMimeContentType = class(TMimeContentHeader_AbstractBaseClass)
  private
    FMainType:  String;
    FSubType:   String;

  protected
    function    MayEncodeParam(const name:string):boolean;     override;
    function    GVerb:string;                                  override;
    procedure   SVerb(const value:string);                     override;

  private
    function    GCharset:string;
    procedure   SCharset(const value:string);

    function    GBoundary:string;
    procedure   SBoundary(const value:string);

    function    GNeedCharset:Boolean;
    function    GIsEncodeable:Boolean;
    function    GIsComposed: Boolean;

  public
    property MainType: String   read FMainType  write FMainType;
    property SubType:  String   read FSubType   write FSubType;

    property Charset:  String   read GCharset   write SCharset;
    property Boundary: String   read GBoundary  write SBoundary;

    property NeedCharset:Boolean read GNeedCharset;
    property IsEncodeable:Boolean read GIsEncodeable;

    property IsComposed: Boolean read GIsComposed;
  end;

{ ------------------ Generic Content Type Functions ------------------ }

function MimeContentTypeNeedCharset(const ctype:string):Boolean;
function MimeContentTypeIsEncodeable(const ctype:string):Boolean;
function MimeContentTypeIsComposed(const ctype:string):Boolean;

procedure MimeContentTypeSplit(const ctype:string; var main,sub:string);

function MimeCreateMultipartBoundary(const seed:string):string;

{ ------------------------- Disposition Types ------------------------ }

  { This class implements the handling of the MIME Content-Disposition }
  { header field contents                                              }
type
  TMimeDisposition = class(TMimeContentHeader_AbstractBaseClass)
  private
    FDispo:     TMimeDispositionType;
  protected
    function    MayEncodeParam(const name:string):boolean;     override;
    function    GVerb:string;                                  override;
    procedure   SVerb(const value:string);                     override;
  public
    property    DispoType: TMimeDispositionType read FDispo write FDispo;
  end;

{ ------------------------ Content Encodings ------------------------- }

type TMimeTransferEncoderStream = class(TCodecStream) {abstract} end;
     TMimeTransferDecoderStream = class(TCodecStream) {abstract} end;

function MimeCreateEncoder(encoding:TMIMEEncoding; IsText: Boolean):TMimeTransferEncoderStream;
function MimeCreateDecoder(encoding:TMIMEEncoding                 ):TMimeTransferDecoderStream;

function DecodeBase64(const s: String):String;
function DecodeQuotedPrintable(const s:string):string;

type TSetOfChar = set of char;
function EncodeQuotedPrintable(const s:string; mustEscape: TSetOfChar): string;

function MimeGetEncodingFromName(const s:string):TMimeEncoding;

{ ----------------------------- Charsets ----------------------------- }

{ ----------------------------- RFC 2047 ----------------------------- }

function RFC2047_Decode(const ss: string; csTo: TMIMECharsets):String;
function RFC2047_Encode(ss: string; csFrom: TMIMECharsets;MaxFirstLen,MaxLen:Integer;EOL:String):String;

{ -------------------------- EOL Conversion -------------------------- }

type
  TMimeEolCodecStream = class(TCodecStream)
  end;

  TMimeSingleCharToCRLFStream = class(TMimeEolCodecStream)
  protected
    cc:char;
  public
    constructor Create(EOLChar:Char);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TMimeCRtoCRLFStream = class(TMimeSingleChartoCRLFStream)
  public
    constructor Create; overload;
  end;

  TMimeLFtoCRLFStream = class(TMimeSingleChartoCRLFStream)
  public
    constructor Create; overload;
  end;

function MimeCreateEOLConverter(Eol:TMimeEol):TCoDecStream;

{ ------------------------} implementation { ------------------------- }

uses
  SysUtils,
  xpglobal,
  crc,
  mime_base64,
  mime_qp,
  typeform;

{ -------------------- Content & Disposition Types ------------------- }

{ TMimeParam }

constructor TMimeParam.Create(Const NValue,NCharset,
  NLanguage: String);
begin
  Value   :=NValue;
  Charset :=NCharset;
  Language:=NLanguage;
end;

{ TMimeContentHeader_AbstractBaseClass }

constructor TMimeContentHeader_AbstractBaseClass.Create(const ctype:String);
begin
  inherited Create;
  FParam := TStringList.Create;
//FParam.CaseSensitive := False;
  AsString := ctype;
end;

destructor TMimeContentHeader_AbstractBaseClass.Destroy;
var i: integer;
begin
  for i:=0 to FParam.Count-1 do
    FParam.Objects[i].Free;
  FParam.Free;
  inherited Destroy;
end;

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

function TMimeContentHeader_AbstractBaseClass.GAsString:String;
var i: Integer;
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

function TMimeContentHeader_AbstractBaseClass.AsFoldedString(MaxFirstLen,
  MaxLen:Integer;EOL:String;UseRFC2231:Boolean):String;
var line_start,line_rem: integer;
    para: string;
    i: integer;
begin
  for i := FParam.Count-1 downto 0 do
    if Length((FParam.Objects[i] as TMimeParam).Value)<=0 then begin
      FParam.Objects[i].Free;
      FParam.Delete(i);
    end;

  line_start := 0;
  line_rem := MaxFirstLen;

  Result := Verb;

  for i:= 0 to FParam.Count-1 do
  begin
    para:=Encode(FParam.Strings[i])+'='+Encode(
      (FParam.Objects[i] as TMimeParam).Value);

    if (Length(result)-Line_start)+Length(para)+3 >= line_rem then
    begin
      result:=result+';'+eol+' ';
      Line_Start:=Length(result)-1;
      Line_Rem  :=MaxLen;
    end else
      result := result+'; ';

    result := result+para;
  end;
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

{ --------------------------- Content Types -------------------------- }

{ TMimeContentType }

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

procedure TMimeContentType.SBoundary(Const Value:String);
begin
  ParamValues['boundary']:=Value;
end;

function TMimeContentType.GBoundary:String;
begin
  Result:=ParamValues['boundary'];
end;

function TMimeContentType.MayEncodeParam(const name:string):boolean;
var name_lc: string;
begin
  name_lc := LowerCase(name);
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

function TMimeContentType.GIsComposed: Boolean;
begin
  result:=MimeContentTypeIsComposed(Verb);
end;

{ ------------------ Generic Content Type Functions ------------------ }

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
    ((m<>'application') or (s<>'mac-binhex40'));
end;

function MimeContentTypeIsComposed(const ctype:string):Boolean;
var m,s: string;
begin
  MimeContentTypeSplit(ctype,m,s);
  m:=Lowercase(m);
  s:=Lowercase(s);

  result := (m='multipart') or ((m='message') and (s='rfc822'));
end;

var boundary_counter: smallword;

function MimeCreateMultipartBoundary(const seed:string):string;
var t,m,j   : smallword;
    h,mm,s,ss: smallword;
    dat     : smallword;
    csum    : smallword;
begin
  decodedate(now,j,m,t);
  decodetime(now,h,mm,s,ss);

  dat:=(t-1)+(m-1)*32+(j mod 165)*32*12;

  csum:=CRC16StrXP(seed);
  inc(boundary_counter);

  result := '=_' + b30(longint(dat) shl 14+boundary_counter shr 2)+
            b30(longint(boundary_counter and 3) shl 28+LongInt(random($1000)) shl 16 +csum);

  while Length(result)<55 do
    result:=result+b30(Longint((random(1 shl 15)shl 15))+Longint(random(1 shl 15)));

  if Length(result)>70 then
    result:=LeftStr(result,70);
end;

{ ------------------------- Disposition Types ------------------------ }

{ TMimeDisposition }

procedure TMimeDisposition.SVerb(Const Value:String);
var s: string;
begin
  s:=lowercase(value);

  if s='inline' then
    FDispo:=MimeDispositionInline
  else if s='attachment' then
    FDispo:=MimeDispositionAttach
  else
    FDispo:=MimeDispositionNone;
end;

function TMimeDisposition.GVerb:String;
begin
  case FDispo of
    MimeDispositionAttach: result:='attachment';
    MimeDispositionInline: result:='inline';
    else result:='inline';
  end;
end;

function TMimeDisposition.MayEncodeParam(const name:string):boolean;
var name_lc: string;
begin
  name_lc := LowerCase(name);
  Result :=
    (name_lc<>'creation-date') and
    (name_lc<>'modification-date') and
    (name_lc<>'read-date') and
    (name_lc<>'size');
end;

{ ------------------------ Content Encodings ------------------------- }

function MimeCreateEncoder(encoding:TMIMEEncoding; IsText: Boolean):TMimeTransferEncoderStream;
begin
  case encoding of
    MimeEncodingBase64:          Result:=TBase64EncoderStream.Create;
    MimeEncodingQuotedPrintable: Result:=TQuotedPrintableEncoderStream.Create(IsText);
    else Result:=nil;
  end;
end;

function MimeCreateDecoder(encoding:TMIMEEncoding):TMimeTransferDecoderStream;
begin
  case encoding of
    MimeEncodingBase64:          Result:=TBase64DecoderStream.Create;
    MimeEncodingQuotedPrintable: Result:=TQuotedPrintableDecoderStream.Create;
    else Result:=nil;
  end;
end;

{ ---------------- Simple Content Encoding Functions ----------------- }
// performance critical!
function DecodeBase64(const s: String):String;
var
  p1: integer;

  function nextbyte: byte;
  var
    p, l: integer;
  begin
    l := Length(s);
    if p1 <= l then
    begin
      repeat
        p := Base64DecodingTable[byte(s[p1])];
        inc(p1);
      until (p >= 0) or (p1 > l);
      if p>=0 then
        Result:=Byte(p)
      else
        Result := 0;
    end else
    begin
      Result := 0;
      exit;
    end;
  end;

var
  b1, b2, b3, b4: byte;
  i, pad: Integer;
begin
  if length(s) >= 3 then
  begin
    if LastChar(s) = '=' then
    begin
      if (Length(s) >= 2) and (s[length(s) - 1] = '=') then
        pad := 2
      else
        pad := 1;
      // falls ein zus�tzliches "=" angeh�ngt wurde, diesen Datensatz verwerfen
      if Length(s) mod 4 <> 0 then
        Pad := 3;
    end else
    begin
      if Length(Trim(s)) mod 4 <> 0 then
      begin
        { kein gueltiger Base64 String }
        DecodeBase64 := s; Exit;
      end else
        pad := 0;
    end;

    p1 := 1; i := 0;
    SetLength(Result, Length(s) * 3 div 4 + 3); // precalulate decoded length
    while p1 <= length(s) do
    begin
      b1 := nextbyte; b2 := nextbyte; b3 := nextbyte; b4 := nextbyte;
      Inc(i);
      Result[i] := chr(b1 shl 2 + b2 shr 4);
      Inc(i);
      Result[i] := chr((b2 and 15) shl 4 + b3 shr 2);
      Inc(i);
      Result[i] :=  chr((b3 and 3) shl 6 + b4);
    end;
    SetLength(Result, i-pad);
  end else
    Result := s;
end;

function DecodeQuotedPrintable_Internal(const s:string; rfc2047:boolean):string;
var p, b: Integer;
  softbrk: boolean;
begin
    result := TrimRight(s);
    softbrk := (lastchar(result) = '=');     { quoted-printable: soft line break }
    if softbrk then DeleteLastChar(result);

    if rfc2047 then                     { RFC 2047: decode '_' to ' ' }
      for p:=1 to length(result) do
        if result[p] = '_' then result[p]:=' ';

    p := cpos('=', result);
    if p > 0 then
      while p < length(result) - 1 do
      begin
        inc(p);
        b := hexval(copy(result, p, 2));
        if b > 0 then
        begin
          result[p - 1] := chr(b);
          delete(result, p, 2);
        end;
        while (p < length(result)) and (result[p] <> '=') do
          inc(p);
      end;
    if not(softbrk or rfc2047) then
      result:=result+#13#10;
end;

function DecodeQuotedPrintable(const s:string):string;
begin
  result:=DecodeQuotedPrintable_Internal(s,false);
end;

function EncodeQuotedPrintable(const s:string; mustEscape: Tsetofchar): string;
var i: integer;
begin
  result := '';

  for i:=1 to Length(s) do
    if s[i] = ' ' then
      result := result + ' ' else  
    if s[i] in mustEscape+['_'] then
      result := result + UpperCase(hex(ord(s[i]),2))
    else
      result := result + s[i];
end;

{ ---------------- Content Encoding Helper Functions ----------------- }

function MimeGetEncodingFromName(const s:string):TMimeEncoding;
var l:string;
begin
  l:=LowerCase(s);
  for result:=Low(TMimeEncoding) to High(TMimeEncoding) do
    if l=LowerCase(MimeEncodingNames[result]) then
      exit;
  result:=MimeEncodingUnknown;
end;

{ ----------------------------- Charsets ----------------------------- }

{ ----------------------------- RFC 2047 ----------------------------- }

function RFC2047_Decode(const ss: string; csTo: TMIMECharsets):String;
var p,q,r: longint;
    e,t:   longint;
    sd:    string;
    s:     string;
    len:   longint; 
    z:     longint;
    i:     longint;
    chk_valid_zcchars: boolean;

label outer;

begin
  chk_valid_zcchars:=false; {im dekodiertem String ungueltige Zeichen eliminieren }
  p:=1;       { current scan position in ss      }
  q:=1;       { start of data not copied into sd }
  r:=1;       { last non-whitespace char in ss   }
  sd:='';

outer:
  while p<=(length(ss)-9) do { 9 = minimum length for =?c?e?t?= }
  begin
    if(ss[p]='=')and(ss[p+1]='?') (* and(not(phrase and qq)) *) then // start marker
    begin
      (* encoded-word = "=?" charset "?" encoding "?" encoded-text "?=" *)
      (*                     ^c          ^e                         ^t  *)

      e:=p+2; while (e<= Length(ss)) and (ss[e]<>'?') do { encoding position }
      begin
        if (e<=length(ss)-5) and (not(ss[e] in [#0..#32,'(',')','<','>','@',
          ';',':','"',',','[',']','?','.','='])) then
          e:=e+1
        else
        begin
          if ss[e]='=' then
            p:=e      { maybe a new start       }
          else
            p:=e+1;   { go ahead with next char }
          goto outer;   { don't decode anything   }
        end;
      end; // while

      e:=e+1; if(not(ss[e] in ['b','B','Q','q']))or(ss[e+1]<>'?')then
      begin
        p:=e;         { not a valid encoding    }
        continue;     { don't decode anything   }
      end;

      t:=e+2; while (t<= Length(ss)) and (ss[t]<>'?') do  { end marker position }
      begin
        if (t<=length(ss)-2) and(not(ss[t] in ['?',#8,#10,#13])) then
          t:=t+1
        else
        begin if length(ss)<t then break; //** fix!
          if ss[t]='?' then
            p:=t-1    { maybe a new start }
          else begin
            p:=t+1;   { go ahead with next char }
            t:=t+1;
          end;
          continue;   { don't decode anything   }
        end;
      end; // while

      (* now copy unencoded text befor encoded-word *)

      if (p>q) and { there is something to copy }
         ( (q=0) or  { we are at the beginning (i.e. there was not already an encoded-word) }
           (r>=q) )  { the last non-white-space character was not before the stop of the last encoded-word }
      then
        sd := sd + RecodeCharset(copy(ss,q,p-q),csCP1252,csTo);

      (* encoded-word = "=?" charset "?" encoding "?" encoded-text "?=" *)
      (*                 ^p              ^e           ^e+2          ^t  *)

      begin
        if ss[e] in ['B','b'] then { base64 }
          sd := sd + RecodeCharset(DecodeBase64(Copy(ss,e+2,t-(e+2))),MimeGetCharsetFromName(Copy(ss,p+2,e-1-(p+2))),csTo)
        else                       { quoted-printable }
           sd := sd + RecodeCharset(DecodeQuotedPrintable_Internal(Copy(ss,e+2,t-(e+2)),true),MimeGetCharsetFromName(Copy(ss,p+2,e-1-(p+2))),csTo);
        chk_valid_zcchars:=true;
      end;

      p:=t+2;
      q:=p;
      Continue;
    end else // start marker found
    begin
      p:=p+1;
      if not(ss[p-1] in [' ',#10,#13,#8]) then
        r:=p;
    end;
  end; // while

  if (q>1) then   { there has actually something been decoded    }
    Result := sd + RecodeCharset(mid(ss,q),csCP1252,csTo)
  else
    Result := RecodeCharset(ss,csCP1252,csTo);

    { HJT: 10.11.06 aus dekodierten RFC-Headerzeilen    }
    { alle Zeichen entfernen, die dazu fuehren wuerden, }
    { dass ein ungueltiger, insbesondere mehrzeiliger   }
    { ZConnect-Header entstuende.                       }
    if chk_valid_zcchars then
    begin
      s := Result;
      len := length(s);
      SetLength(result, len); 
      z := 0;
      for i := 1 to len do begin
        if s[i] in [#0,#10,#13,#26] then begin
          Debug.DebugLog('mime.pas', 'remove_RFC2047_Decode, removed '
                          +Hex(Ord(s[i]),2)+' from header: '
                          +'<'+s+'>', DLInform);
        end
        else begin
          inc(z,1);
          Result[z] := s[i];
        end;
      end;
      SetLength(Result, z);
    end;
end;

function RFC2047_Encode(ss: string; csFrom: TMimeCharsets; MaxFirstLen,MaxLen:Integer;EOL:String):String;
var pos:   integer; // current scan position
    enc,noascii,first:   boolean;
    ds: string;     // recoded string
    csTo:  TMimeCharsets;

  procedure psq(x:string);
  begin
    if (Length(x)>MaxFirstLen-2) and (not first) then
    begin
      Result:=Result+'?='+EOL+' =?'+MimeGetCharsetName(csTo)+'?Q?';
      MaxFirstLen := Min(76,MaxLen) - 6 - Length(MimeGetCharsetName(csTo));
    end;
    Result := Result + x;
    Dec(MaxFirstLen,Length(x));
    first := false;
  end;

  procedure pec(c:char);
  begin
    psq('='+hex(ord(c),2));
  end;

  procedure pu8(const x:string;pos:Integer);
  var x2:string;
  begin
    x2:='='+hex(ord(x[pos]),2);
    inc(pos);
    while (x[pos] in [#$80..#$BF]) and (pos<=Length(x)) do
      x2:=x2+'='+hex(ord(x[pos]),2);
    psq(x2);
  end;

begin
  ds:=RecodeCharset(ss,csFrom,csUTF8);          // convert to UTF-8
  enc:=false;
  noascii:=false;
  result:='';

  for pos:=1 to Length(ds) do
    if (ds[pos] in [#$80..#$FF]) then
    begin
      enc := true;
      noascii := true;
      break;
    end else
    if (ds[pos] in [#0..#$21]) or
      ((pos>1) and ( ((ds[pos-1]='=') and (ds[pos]='?')) or
                     ((ds[pos-1]='?') and (ds[pos]='=')) ) ) then
    begin 
      enc:=true;
    end;

  if enc then
  begin
    if not noascii then
    begin
      ss := ds;
      csTo:=csASCII;
    end else
    begin
      ss:=RecodeCharset(ds,csUTF8,csISO8859_1);   // convert to ISO-8859-1
      if RecodeCharset(ss,csISO8859_1,csUTF8)=ds then // ISO-8859-1 works!
      begin
        ds:=ss;
        csTo:=csISO8859_1;
      end else
      begin
        ss:=RecodeCharset(ds,csUTF8,csISO8859_15);   // convert to ISO-8859-15
        if RecodeCharset(ss,csISO8859_15,csUTF8)=ds then // ISO-8859-15 works!
        begin
          ds:=ss;
          csTo:=csISO8859_15;
        end else
          csTo:=csUTF8;
      end;
    end;

    MaxFirstLen := Min(76,MaxFirstLen);

    Result:='=?'+MimeGetCharsetName(csTo)+'?Q?';
    Dec(MaxFirstLen,5+Length(MimeGetCharsetName(csTo)));

    first := true;        

    for pos:=1 to length(ds) do
    begin
      if ds[pos]=' ' then
        psq('_') else
      if ds[pos] in ['(',')','=','?','_','"',#0..#$1F] then
        pec(ds[pos]) else
      if(ds[pos] in [#$C0..#$FF]) and (csTo=csUTF8) then // UTF-8 starters
        pu8(ds,pos) else
      if(ds[pos] in [#$80..#$BF]) and (csTo=csUTF8) then // UTF-8 continuations
        begin end else
      if ds[pos] in [#$80..#$FF] then
        pec(ds[pos])
      else
        psq(ds[pos]);
    end;

    Result:=Result+'?=';
    Dec(MaxFirstLen,2);
  end else // !enc
  begin
    Result:=ss;
  end;
end;

{ -------------------------- EOL Conversion -------------------------- }

{ TMimeSingleChartoCRLFStream }

constructor TMimeSingleChartoCRLFStream.Create(eolchar:char);
begin
  inherited Create;
  cc:=eolchar;
end;

function TMimeSingleChartoCRLFStream.Write(const Buffer; Count: Longint): Longint;
var start,pos,written: Longint;
const crlf: array[0..1] of char = (#13,#10);

  procedure WriteTo(pos:Longint);
  begin
    if start>pos then exit;
    Inc(written,OtherStream.Write((PChar(@Buffer)+Start)^,pos-start+1));
    start := pos+1;
  end;

begin
  start := 0;
  written := 0;

  for pos := 0 to Count-1 do
    if (PChar(@Buffer)+Pos)^=cc then
    begin
      if cc=crlf[0] then
      begin
        WriteTo(pos);
        inc(written,OtherStream.Write(crlf[1],1)-1);
      end else
      if cc=crlf[1] then
      begin
        WriteTo(pos-1);
        inc(written,OtherStream.Write(crlf[0],1)-1);
      end else
      begin
        WriteTo(pos-1);
        inc(start);
        inc(written,OtherStream.Write(crlf[0],2)-1);
      end;
    end;

  WriteTo(Count-1);

  Result:=Count;
end;

{ TMimeCRtoCRLFStream }

constructor TMimeCRtoCRLFStream.Create;
begin inherited Create(#13); end;

{ TMimeLFtoCRLFStream }

constructor TMimeLFtoCRLFStream.Create;
begin inherited Create(#10); end;

function MimeCreateEOLConverter(Eol:TMimeEol):TCodecStream;
begin
  case EOL of
    MimeEOLCR: Result:=TMimeCRtoCRLFStream.Create;
    MimeEOLLF: Result:=TMimeLFtoCRLFStream.Create;
    else Result := TNullCodecStream.Create;
  end;
end;

//
// $Log: mime.pas,v $
// Revision 1.29  2003/10/21 21:25:04  cl
// - Changed THeader.MIME to use TMimeContentType and TMimeDisposition objects
// - Changed MausTausch headers for Maus-internal IDs: MID/BEZ => maus_*, org_* => MID/BEZ,
//
// Revision 1.28  2003/09/29 20:47:12  cl
// - moved charset handling/conversion code to xplib
//
// Revision 1.27  2003/08/26 22:47:16  cl
// - split xpstreams into individual small files to remove some dependencies
//
// Revision 1.26  2003/08/24 19:12:33  cl
// - better aliases for charsets
// - additional aliases for charsets not registered w/ IANA (e.g. x-mac-roman)
//
// Revision 1.25  2003/01/07 00:22:28  cl
// - made parameter const
//
// Revision 1.24  2002/12/14 07:31:28  dodi
// - using new types
//
// Revision 1.23  2002/12/04 16:56:59  dodi
// - updated uses, comments and todos
//
// Revision 1.22  2002/11/14 20:04:41  cl
// - New charset: ISO-646-DE
//
// Revision 1.21  2002/05/20 15:18:17  cl
// - added/fixed functions for proper RFC 2047 encoding
//
// Revision 1.20  2002/05/14 07:44:57  mk
// *** empty log message ***
//
// Revision 1.19  2002/04/14 22:11:11  cl
// - added EncodeQuotedPrintable
// - fixes for RFC2047_Decode
//
// Revision 1.18  2002/03/22 18:22:12  ml
// - kylix 2 compilable
//
// Revision 1.17  2002/02/25 17:49:16  mk
// - faster DecodeBase64
//
// Revision 1.16  2002/02/22 18:29:59  cl
// - added windows-1250
//
// Revision 1.15  2002/02/18 16:56:26  cl
// - added TMIMEContenttype.IsComposed and MimeTypeIsComposed
//
// Revision 1.14  2002/01/04 23:24:24  cl
// - added MS-DOS Codepage 857 (Multilingual Latin 5)
//
// Revision 1.13  2002/01/04 22:34:32  cl
// - added IBM codepages 850 and 858
// - moved Get8BitTable to unit charmaps
//
// Revision 1.12  2002/01/03 20:53:54  cl
// - added ISO-8859-16
//
// Revision 1.11  2001/12/30 19:56:48  cl
// - Kylix 2 compile fixes
//
// Revision 1.10  2001/12/30 18:15:10  cl
// - additional fixes/adaptions for last commit
//
// Revision 1.9  2001/11/28 09:35:51  mk
// - fixed range check error
//
// Revision 1.8  2001/09/10 17:24:26  cl
// - BUGFIX: return value of TMimeSingleChartoCRLFStream.Write
//
// Revision 1.7  2001/09/10 15:58:01  ml
// - Kylix-compatibility (xpdefines written small)
// - removed div. hints and warnings
//
// Revision 1.6  2001/09/09 17:37:19  cl
// - moved common code between alle en-/decoding streams to a base class
// - all en-/decoding streams can now destruct the other stream
// - much more elegant way to connect en-/decoding streams to each other
//
// -  fixed hang in RFC2047_Decode if encoded-word contained on of [' ',#8,#10,#13]
//
// Revision 1.5  2001/09/09 10:23:20  ml
// - Kylix compatibility stage III
// - compilable in linux
//
// Revision 1.4  2001/09/08 20:57:27  cl
// - unencoded 8bit chars in RFC header lines now treted as Windows-1252 (=Windows Quirks)
//
// Revision 1.3  2001/09/08 18:46:43  cl
// - small bug/compiler warning fixes
//
// Revision 1.2  2001/09/08 16:29:30  mk
// - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
// - some AnsiString fixes
//
// Revision 1.1  2001/09/08 15:03:18  cl
// - Moved MIME functions/types/consts to mime.pas
//

{ ----------------------------------------------------------------} end.
