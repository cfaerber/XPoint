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

{$I xpdefine.inc }

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

{ ------------------------ RFC 2822 Addresses ------------------------ }

function RFCIsValidAddress(const addr:string): boolean;
function RFCNormalizeAddress(const addr,domain:string):string;

{ ------------------------} implementation { ------------------------- }

uses
  Typeform,
{$IFDEF Delphi}
  {$IFDEF Kylix}
  IdGlobal,
  {$ELSE}
  strutils,
  {$ENDIF}
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

{ ------------------------ RFC 2822 Addresses ------------------------ }

// addr-spec       =       local-part "@" domain
// local-part      =       dot-atom / quoted-string / obs-local-part
// domain          =       dot-atom / domain-literal / obs-domain
// domain-literal  =       [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]
// dcontent        =       dtext / quoted-pair
// atext           =       ALPHA / DIGIT / ; Any character except controls,
//                         "!" / "#" /     ;  SP, and specials.
//                         "$" / "%" /     ;  Used for atoms
//                         "&" / "'" /
//                         "*" / "+" /
//                         "-" / "/" /
//                         "=" / "?" /
//                         "^" / "_" /
//                         "`" / "{" /
//                         "|" / "}" /
//                         "~"
// atom            =       [CFWS] 1*atext [CFWS]
// dot-atom        =       [CFWS] dot-atom-text [CFWS]
// dot-atom-text   =       1*atext *("." 1*atext)
// qtext           =       NO-WS-CTL /     ; Non white space controls
//                         %d33 /          ; The rest of the US-ASCII
//                         %d35-91 /       ;  characters not including "\"
//                         %d93-126        ;  or the quote character
// qcontent        =       qtext / quoted-pair
// quoted-string   =       [CFWS]
//                         DQUOTE *([FWS] qcontent) [FWS] DQUOTE
//                         [CFWS]

function RFCIsValidAddress(const addr:string): boolean;
var i,p:integer;
    q : boolean; { in a quoted-string/domain-literal                   }
    d : boolean; { directly after a dot or at start (not counting FWS) }
    aq: boolean; { directly after a quoted-string   (not counting FWS) }
    aw: boolean; { after whitespace                                    }
    a : boolean; { at the very beginning            (not counting FWS) }
    h : boolean; { after a hyphen                                      }
begin
  // by setting result to false, we can just exit on any error found
  result:=false;
  i:=1;

  q :=false;
  aq:=false;
  aw:=false;
  
  // There must not be a dot at the start of the local-part
  // NB: this also kills empty local parts, which is a good thing
  d:=true;

  while i<=length(addr) do
  begin
    // found a quote  
    if addr[i]='"' then 
    begin
      // a quoted string MUST start after a dot (or at the very start)
      if not (d or q) then exit;
      // set the after-quote flag (if end quote)
      if q then aq:=true;      
      // switch the quote flag     
      q:=not q;
      // and we are not after a dot (or at the beginning)
      d:=false;
      aw:=false;
    end else
    if q then
    begin
      // quoted-pair: ignore next char (error if we're at the end)
      if addr[i]='\' then if i=length(addr) then exit else inc(i) else
      // in quotes, most characters are allowed      
      if not (addr[i] in [
        #1..#8,#11,#12,#14..#31,        // NO-WS-CTL \ qtext
        #33,#35..#91,#93..#126,#127,    //           / 
        ' ',#9,                         // WSP
        #13,#10] ) then exit;           // CR, LF
    end else
    begin
      // we found an (unquoted) at-sign, local-part finished    
      if addr[i]='@' then break else
      // a dot is not allowed at the start, end or after another dot      
      if addr[i]='.' then if d then exit else begin d:=true; aq:=false; aw:=false end else 
      // FWS is ignored
      if addr[i] in [' ',#9,#13,#10] then aw:=true else 
      // all other characters must not come after a quoted-string
      if aq or (aw and not d) then exit else      
      // valid characters in       
      if not (addr[i] in ['A'..'Z','a'..'z','0'..'9',
        '!','#','$','%','&',#$27,'*','+','-','/','=','?',
        '^','_','`','{','|','}','~']) then exit else begin d:=false; aq:=false; aw:=false; end;
    end;

    inc(i);
  end;

  // There must not be a dot at the end of the local-part
  // and quoted-strings must have been closed
  if d or q then exit;

  d := true;
  a := true;
  q := false;
  aq:= false;
  aw:= false;
  h := false;
  
  inc(i);
  while i<=length(addr) do
  begin  
    if (addr[i]='[') and not (q or aq) then
    begin
      // a [ must be at the start
      if not a then exit else
      // set the domain-literal flag
      q:=true;
    end else
    if q then 
    begin
      // quoted-pair: next character ignored
      if addr[i]='\' then if i=length(addr) then exit else inc(i) else
      if addr[i]=']' then begin q:=false; aq:=true; h:=false; end else
      if not (addr[i] in [
        #1..#8,#11,#12,#14..#31,        // NO-WS-CTL \ dtext
        #33..#90,#94..#126,#127,        //           / 
        ' ',#9,                         // WSP
        #13,#10] ) then exit;           // CR, LF
    end else
    begin
      // FWS is ignored
      if addr[i] in [' ',#9,#13,#10] then if h then exit else aw:=true else 
      // all other characters must not come after a domain literal
      if aq then exit else
      // a dot is not allowed at the start, end or after another dot      
      if addr[i]='.' then if d or h then exit else begin d:=true; a:=false; aw:=true; end else 

      // all other characters MUST not come after a whitespace w/o dot
      if aw and not d then exit else

      // NB: RFC 2822 allows more characters, but these can not be valid
      // host names according to RFC 0882

      // a hyphen must not be at the beginning of a label
      if addr[i] = '-' then if d then exit else begin h:=true; d:=false; a:=false; aw:=false; end else

      // NB: RFC 0882 does not allow digits at the beginning of a label,
      // which NSI does not care about, so we don't either
      if not(addr[i] in ['A'..'Z','a'..'z','0'..'9'])then exit else begin d:=false; a:=false; h:=false; aw:=false; end;
    end;

    inc(i);
  end;

  // A hyphen MUST not be at the end
  // A domain-literal MUST have been closed
  // NB: But we do allow a '.' at the end of a domain name!
  if h or q then exit;
  
  result:=true;
end;

function RFCNormalizeAddress(const addr,domain:string):string;
var i,p: integer;
    q: boolean; { in quoted string  }    
    s: boolean; { after a backslash }
begin
  result:='';

  p:=-1;

  s:=false;
  q:=false;
  
  for i:=1 to Length(addr) do
  begin
    if s then 
    begin 
      s:=false; 
      // \-escape, if necessary
      if not (addr[i] in [#1..#8,#9,#11,#12,#14..#31,#33,#35..#91,#92..#126]) then result:=result+'\';
      result:=result+addr[i] 
    end else
    if addr[i]='"' then q:=not q else
    if addr[i]='\' then s:=true else
    begin
      // set a mark at the last @ sign
      if not q and(addr[i]='@') then p:=Length(result);
      // Unquoted FWS is ignored      
      if q or not (addr[i] in [#0..#32]) then
      begin
        // \-escape, if necessary
        if not (addr[i] in [#1..#8,#9,#11,#12,#14..#31,#33,#35..#91,#92..#126]) then result:=result+'\';
        result:=result+addr[i];
      end;
    end;
  end;

  if p<0 then 
  begin
    p:=Length(result);
    result:=result+'@'+domain;
  end;
  
  q:=false;

  for i:=1 to p do
    if not (result[i] in ['.','A'..'Z','a'..'z','0'..'9',
        '!','#','$','%','&',#$27,'*','+','-','/','=','?',
        '^','_','`','{','|','}','~']) then begin q:=true; break; end;
  
  if (p=0) or ('.' in [addr[1],addr[p]]) then q:=true;

  if q then result:='"'+Copy(result,1,p)+'"'+Mid(result,p+1);
end;

{ ----------------------------------------------------------------} end.

