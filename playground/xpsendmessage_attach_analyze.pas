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

unit xpsendmessage_attach_analyze;

{ ---------------------------} interface { --------------------------- }

uses
  mime, xpsendmessage_attach;

procedure SendAttach_Analyze(pa:TMIME_Part;Change:Boolean);

{ ------------------------} implementation { ------------------------- }

uses
{$IFDEF unix}
  xpcurses,
{$ENDIF}
  classes, database, fileio, inout, keys, lister, maus2, resource, typeform,
  winxp, xp0, xp1, xp1input, xp1o, xp3, xp3o, xp4e, xpe, xpglobal, xpnt, maske,
  sysutils, windows;

function GuessContentTypeFromFileName(FileName:String):String;
var ext: string;
    i:   Integer;
{$IFDEF Win32}
    reg: HKEY;
    value_data: String;
    value_size: DWORD;
    reg_res: LONG;
{$ENDIF}
begin
  Result := '';

  { Read MIME type from database                           }

  ext := ExtractFileExt(FileName);
  dbSeek(mimebase, mtiExt, UpperCase(Mid(ext,2)));
  if dbFound then
    Result := DBReadNStr(MimeBase,MimeB_typ);

  { Ignore wildcard entries in database                    }

  for i:=1 to Length(Result) do
    if Result[i] in [';','/'] then
      break
    else if Result[i] in ['*'] then begin
      Result := ''; break; end;

{$IFDEF Win32} 
  if (Result='') and (RegOpenKeyEx(HKEY_CLASSES_ROOT, PChar(Ext), 0,
    KEY_QUERY_VALUE, @reg) = ERROR_SUCCESS) then
  begin
    SetLength(value_data,255);
    value_size:=Length(value_data);
    reg_res := RegQueryValueEx(reg,'Content Type',nil,nil,@(value_data[1]),@value_size);
    if reg_res = ERROR_MORE_DATA then
    begin
      SetLength(value_data,value_size);
      reg_res := RegQueryValueEx(reg,'Content Type',nil,nil,@(value_data[1]),@value_size);
    end;
    if reg_res = ERROR_SUCCESS then
    begin
      SetLength(value_data,value_size+iif(value_data[value_size]=#0,-1,0));
      Result:=value_data;
    end;
  end;
{$ENDIF}
end;
      
procedure SendAttach_Analyze(pa:TMIME_Part;Change:Boolean);
var f:   TFileStream;
    ext: String;
    i,n: Integer;
    c,LastChar:   Char;
    Buffer:     Array[1..8192] of Char;

    GuessedType:	String;
    GuessedCharset:	String;
    GuessedEncoding:	String;

//  HasEOL:     Array[eol_cr,eol_lf,eol_crlf] of Integer; (* Number of line end types *)
//  HasByte:    Array[0..255]                 of Integer; (* Number of byte values    *)
//  HasUTF8:    Array[boolean]                of Integer; (* Number of UTF-8 sequences/non-UTF-8 8bit values *)

//  MaxLineLen: Array[eol_cr,eol_lf,eol_crlf] of Integer;
//  CurLineLen: Array[eol_cr,eol_lf,eol_crlf] of Integer;

//procedure SetLineLength(eol_type: (eol_cr,eol_lf,eol_crlf); 
//  chars_before,chars_after: Integer);
//begin
//  CurLineLen:=CurLineLen+chars_before;
//  if CurLineLen>MaxLineLen then MaxLineLen:=CurLineLen;
//  CurLineLen:=chars_after;
//end;

begin

  GuessedType := GuessContentTypeFromFileName(
    iifs(pa.IsFile,pa.FileName,pa.FileNameO));

//	  { analyze the data to determine possible encodings       }
//	  f := TFileStream(pa.FileName,fmRead);
//	
//	  CurLineLen:=0;
//	  MaxLineLen:=0;
//	
//	  n := f.Read(Buffer,Sizeof(Buffer));
//	
//	  while n>0 do
//	  begin
//	    for i:=1 to n do 
//	    begin 
//	      c:=Buffer[i];
//	      Inc(CurLineLen[eol_CRLF]);
//	      Inc(CurLineLen[eol_LF]);
//	      Inc(CurLineLen[eol_CR]);
//	
//	      { check for line endings                             }
//	      if c=#10 then begin
//	        if Lastchar=#13 then begin
//	          Inc(HasEOL[eol_CRLF]);
//		  MaxLineLen[eol_CRLF]:=Max(CurLineLen[eol_CRLF]-2,MaxLineLen[eol_CRLF]);
//		  CurLineLen[eol_CRLF]:=0;
//		  MaxLineLen[eol_LF  ]:=Max(CurLineLen[eol_LF  ]-1,MaxLineLen[eol_LF  ]);
//		  CurLineLen[eol_LF  ]:=0;
//		  MaxLineLen[eol_CR  ]:=Max(CurLineLen[eol_CR  ]-2,MaxLineLen[eol_CR  ]);
//		  CurLineLen[eol_CR  ]:=1;
//		end
//	        else begin
//	          Inc(HasEOL[eol_LF]);
//		  MaxLineLen[eol_LF  ]:=Max(CurLineLen[eol_LF  ]-1,MaxLineLen[eol_LF  ]);
//		  CurLineLen[eol_LF  ]:=0;
//		end;
//	      end else 
//	        if Lastchar=#13 then begin
//	          Inc(HasEOL[eol_CR]);
//		  MaxLineLen[eol_CR  ]:=Max(CurLineLen[eol_CR  ]-2,MaxLineLen[eol_CR  ]);
//		  CurLineLen[eol_CR  ]:=1;
//		end;
//	    
//	      LastChar:=c;
//	    end;
//	    n := f.Read(Buffer,Sizeof(Buffer));
//	  end;
//	  
//	  f.Free;
//	
  if Change then
  { change all parameters to the values determined through }
  { our analyzation					   }
  begin
    pa.ContentType := iifs(Length(GuessedType)>0,GuessedType,'application/octet-stream');

  end else 
  { only change those parameters where we would otherwise  }
  { create illegal messages 				   }
  begin

  end;
end;

end.
