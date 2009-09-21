{ $Id: mime.pas,v 1.29 2003/10/21 21:25:04 cl Exp $

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

unit mime_rfc2047;

{ ---------------------------} interface { --------------------------- }

uses
  Classes,
  xpcharset;

type TRFC2047Encoding = (
  rfc2047_display,
  rfc2047_phrase,
  rfc2047_text,
  rfc2047_comment );  

function RFC2047_Recode(const source: string; source_charset: TMimeCharsets;
  dest_charset: TMimeCharsets; dest_encoded_charsets: array of TMimeCharsets;
  var MaxLenFirst: integer; MaxLen: integer;
  source_type, dest_type: TRFC2047Encoding): string;

{ ------------------------} implementation { ------------------------- }

uses
  typeform,
  mime,
  xpunicode,
  xpcharset_info;

(*
  phrase          =       1*word / obs-phrase
  obs-phrase      =       word *(word / "." / CFWS)
  word            =       atom / quoted-string

  text            =       %d1-9 /         ; Characters excluding CR and LF
                          %d11 /
                          %d12 /
                          %d14-127 /
                          obs-text

  comment         =       "(" *([FWS] ccontent) [FWS] ")"
  ccontent        =       ctext / quoted-pair / comment
  ctext           =       NO-WS-CTL /     ; Non white space controls

                          %d33-39 /       ; The rest of the US-ASCII
                          %d42-91 /       ;  characters not including "(",
                          %d93-126        ;  ")", or "\"
*)

function _decode(const ss: utf8string; language_tags: boolean): string;
var p,q,r: longint;
    e,t:   longint;
    sd,dd,cs: string;
    lang:   string;
    s:     integer;

label outer;

begin
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
        sd := sd + Copy(ss,q,p-q);

      (* encoded-word = "=?" charset "?" encoding "?" encoded-text "?=" *)
      (*                 ^p              ^e           ^e+2          ^t  *)

      begin
        if ss[e] in ['B','b'] then { base64 }
          dd := DecodeBase64(Copy(ss,e+2,t-(e+2)))
        else                       { quoted-printable }
          dd := DecodeQuotedPrintable_Internal(Copy(ss,e+2,t-(e+2)),true);
        cs := Copy(ss,p+2,e-1-(p+2));
        s := CPos('*',cs);

        if s>0 then begin
          lang := Mid(cs,s+1);
          SetLength(cs,s-1);
        end;

        if language_tags and (s>0) then sd := sd + UTF8Char($E0001) + UTF8Tag(lang);
        sd := sd + RecodeCharset(dd,MimeGetCharsetFromName(cs),csUTF8);
        if language_tags and (s>0) then sd := sd + UTF8Char($E007F) + UTF8Char($E0001);
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
    Result := sd + RecodeCharset(mid(ss,q),csCP1252,csUTF8)
  else
    Result := ss;
end;

function RFC2047_Recode(const source: string; source_charset: TMimeCharsets;
  dest_charset: TMimeCharsets; dest_encoded_charsets: array of TMimeCharsets;
  var MaxLenFirst: integer; MaxLen: integer;
  source_type, dest_type: TRFC2047Encoding): string;
var
  source_u8,
  result_u8,
  data:         UTF8String;

  source_info,
  dest_info:    TCharsetInfo;

  i,j:          integer;

begin
  source_info := nil;
  dest_info := nil;
  try
    source_u8 := RecodeCharset(source, source_charset, csUTF8);
    dest_info := TCharsetInfo.Create(dest_charset);
    source_info := TCharsetInfo.Create(source_charset);

    if source_type <> rfc2047_display then
      data := _decode(source_u8, true);

    result_u8 := data;

    result := RecodeCharset(result_u8, csUTF8, dest_charset);
  finally
    source_info.Free;
    dest_info.Free;
  end;
end;

end.

