{   $Id$

    Copyright (C) 2000-2001 OpenXP team (www.openxp.de) and Claus F"arber

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

{ Nachrichten versenden: Attachments                                   }

{$I xpdefine.inc}

unit xpsendmessage_attach_analyzer;

{ ---------------------------} interface { --------------------------- }

uses
  mime;

procedure SendAttach_Analyze(pa:TMIME_Part;Change:Boolean);

{ ------------------------} implementation { ------------------------- }

uses
{$IFDEF unix}
  xpcurses,
{$ENDIF}
  inout, keys, lister, maus2, resource, typeform, winxp, xp0, xp1, xp1input,
  xp1o, xp3, xp3o, xp4e, xpe, xpglobal, xpnt, maske;

procedure SendAttach_Analyze(pa:TMIME_Part;Change:Boolean);
var f:   TFileStream;
    ext: String;
    i,n: Integer;
    c,LastChar:   Char;
    Buffer:     Array[1..8192] of Char;

    GuessedType:	String;
    GuessedCharset:	String;
    GuessedEncoding:	String;

    HasEOL:     Array[eol_cr,eol_lf,eol_crlf] of Integer; (* Number of line end types *)
    HasByte:    Array[0..255]                 of Integer; (* Number of byte values    *)
    HasUTF8:    Array[boolean]                of Integer; (* Number of UTF-8 sequences/non-UTF-8 8bit values *)

    MaxLineLen: Integer;
    CurLineLen: Integer;

  procedure SetLineLength(eol_type: (eol_cr,eol_lf,eol_crlf); 
    chars_before,chars_after: Integer);
  begin
    CurLineLen:=CurLineLen+chars_before;
    if CurLineLen>MaxLineLen then MaxLineLen:=CurLineLen;
    CurLineLen:=chars_after;
  end;

begin
  { !!TODO: read metainformation from filesystem on        }
  {         platforms where it is available                }

  GuessedType := '';

  { Read MIME type from database                           }

  ext := ExtractFileExt(iifs(pa.IsFile,pa.FileName,pa.FileNameO));
  dbSeek(mimebase, mtiExt, UpperCase(Mid(
    ExtractFileExt(iifs(pa.IsFile,pa.FileName,pa.FileNameO)),2)));
  GuessedType := DBReadNStr(MimeBase,MimeB_typ);

  { Ignore wildcard entries in database                    }

  for i:=1 to Length(GuessedType) do
    if GuessedType[i] in [';','/'] then
      break
    else if GuessedType[i] in ['*'] then begin
      GuessedType:=''; break; end;

  { analyze the data to determine possible encodings       }

  f := TFileStream(pa.FileName,fmRead);

  CurLineLen:=0;
  MaxLineLen:=0;

  n := f.Read(Buffer,Sizeof(Buffer));

  while n>0 do
  begin
    for i:=1 to n do 
    begin 
      c:=Buffer[i];

      { check for line endings                             }
      if c=#10 then begin
        if Lastchar=#13 then
          Inc(HasEOL[eol_CRLF])
        else
          Inc(HasEOL[eol_LF]);
      end else 
        if Lastchar=#13 then
          Inc(HasEOL[eol_CR]);
    
      LastChar:=c;
      Inc(CurLineLen);
    end;
    n := f.Read(Buffer,Sizeof(Buffer));
  end;
  
  f.Free;

  if Change then
  { change all parameters to the values determined through }
  { our analyzation					   }
  begin

  end else 
  { only change those parameters where we would otherwise  }
  { create illegal messages 				   }
  begin

  end;
end;

end.
